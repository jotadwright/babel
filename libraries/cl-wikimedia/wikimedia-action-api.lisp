
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

;; Wikimedia Action API: see https://www.mediawiki.org/wiki/API:Main_page for documentation.
;; Also check this sandbox to see what is supported: https://www.mediawiki.org/wiki/Special:ApiSandbox

(export '(wikimedia-action-api
          wikipedia-query wikipedia-search wikidata-query wikidata-search
          wikidata-get-entity wikidata-get-entity-statements wikidata-get-statement
          wikimedia-paraminfo
          wikipedia-parse))

;;=========================================================================
;; BASIC FUNCTION FOR ALL USES
;;=========================================================================
;;
;; Everything you want to do can be done directly with the macro wikimedia-action-api. 
;; Other functions in this file are simply provided for convenience's sake.

(defun handle-parameters (parameters)
  "Helper function for turning correctly formatting keywords into drakma parameters."
  (if (null parameters)
    nil
    (cons (cons (format nil "~(~a~)" (first parameters))
                (second parameters))
          (handle-parameters (cddr parameters)))))
;; (handle-parameters '(:action "search" :language "en"))

(defun wikimedia-action-api (api-endpoint &rest parameters
                                          &key &allow-other-keys)
  "Function for making requests to the wikimedia-action-api."
  ;; Get the keywords that are not part of the request's parameters:
  (destructuring-bind (&whole whole
                              &key (method :get method-p)
                              (user-agent *user-agent* user-agent-p)
                              (content-type "application/json" content-type-p)
                              (lisp-format :alist lisp-format-p)
                              &allow-other-keys)
      parameters
    ;; Remove them from the parameters' property list
    (dolist (indicator '(:method :user-agent :content-type :lisp-format))
      (remf whole indicator))
    ;; Perform the http-request:
    (let ((response-stream (drakma:http-request api-endpoint
                                                :user-agent user-agent
                                                :method method
                                                :parameters (handle-parameters 
                                                             (append whole (list :format "json")))
                                                :content-type content-type
                                                :want-stream t)))
      ;; Format and parse the response stream:
      (setf (flexi-streams:flexi-stream-external-format response-stream) :utf-8)
      (yason:parse response-stream :object-as lisp-format))))

;;=========================================================================
;; Convenience Functions for QUERYING
;;=========================================================================

;; The query module has three submodules:
;; - META-information: about the wiki and the logged-in user
;; - PROPERTIES of pages, including page revisions and content
;; - LISTS of pages that match certain criteria
;; ------------------------------------------------------------------------

;;=========================================================================
;; Convenience Macro for QUERYING
;;=========================================================================

;; The query module has three submodules:
;; - META-information: about the wiki and the logged-in user
;; - PROPERTIES of pages, including page revisions and content
;; - LISTS of pages that match certain criteria
;; ------------------------------------------------------------------------

(defmacro wikipedia-query (&rest parameters 
                                 &key &allow-other-keys)
  (destructuring-bind (&whole whole 
                              &key (language "en" language-p)
                              &allow-other-keys)
      parameters
    (remf whole :language)
    `(let ((uri (format nil "https://~a.wikipedia.org/w/api.php" ,language)))
       (wikimedia-action-api uri :action "query" ,@whole))))

(defmacro wikipedia-search (search-string &rest parameters
                                          &key &allow-other-keys)
  `(wikipedia-query :list "search" :srsearch ,search-string ,@parameters))
;; (wikipedia-search "Venus")
;; (wikipedia-search "Venus" :language "nl")
;; (wikipedia-query :titles "Albert Einstein" :prop "categories")
;; (wikipedia-query :titles "Venus")

(defmacro wikidata-query (&rest parameters 
                                &key &allow-other-keys)
  `(wikimedia-action-api "https://wikidata.org/w/api.php" 
                         :action "query" ,@parameters))
;; (wikidata-query :titles "Q47652" :prop "description")

(defmacro wikidata-search (search-string &rest parameters
                                         &key &allow-other-keys)
  `(wikidata-query :list "search" :srsearch ,search-string ,@parameters))
;; (wikidata-search "Venus")

(defmacro wikidata-get-entity (entity-id &rest parameters
                                         &key &allow-other-keys)
  `(wikimedia-action-api (format nil "https://wikidata.org/w/rest.php/wikibase/v0/entities/items/~a" ,entity-id)
                         ,@parameters))
;; (wikidata-get-entity "Q47652" :user-agent "YOURUSERAGENT")

(defmacro wikidata-get-entity-statements (entity-id &rest parameters
                                         &key &allow-other-keys)
  `(wikimedia-action-api (format nil "https://wikidata.org/w/rest.php/wikibase/v0/entities/items/~a/statements" ,entity-id)
                         ,@parameters))
;; (wikidata-get-entity-statements "Q47652" :user-agent "YOURUSERAGENT")

(defmacro wikidata-get-statement (statement-id &rest parameters
                                               &key &allow-other-keys)
  `(wikimedia-action-api (format nil "https://wikidata.org/w/rest.php/wikibase/v0/statements/~a" ,statement-id)
                         ,@parameters))
;; (wikidata-get-statement "Q47652$1491D330-19A6-4344-A752-2C8175D8C23A" :user-agent "YOURUSERAGENT")

;;=========================================================================
;; Convenience Macro for information about parameter information
;;=========================================================================

(defmacro wikimedia-paraminfo (&rest parameters 
                                     &key &allow-other-keys)
  (destructuring-bind (&whole whole 
                              &key (language "en" language-p)
                              &allow-other-keys)
      parameters
    (remf whole :language)
    `(let ((uri (format nil "https://~a.wikipedia.org/w/api.php" ,language)))
       (wikimedia-action-api uri :action "paraminfo" ,@whole))))

;;=========================================================================
;; Convenience Macro for parsing wikipedia pages
;;=========================================================================

(defmacro wikipedia-parse (&optional &rest parameters 
                                     &key &allow-other-keys)
  (destructuring-bind (&whole whole 
                              &key (language "en" language-p)
                              &allow-other-keys)
      parameters
    (remf whole :language)
    `(let ((uri (format nil "https://~a.wikipedia.org/w/api.php" ,language)))
       (wikimedia-action-api uri :action "parse" ,@whole))))

(defmacro wikipedia-parse-summary (title &rest parameters
                                         &key &allow-other-keys)
  `(wikipedia-parse :summary ,title :prop "" ,@parameters))
