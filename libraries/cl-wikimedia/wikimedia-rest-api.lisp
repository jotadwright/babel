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

(export '(wikimedia-rest-api
          ;; For getting summary data:
          wikimedia-summary
          wikimedia-summary-content-urls wikimedia-summary-content-urls-desktop wikimedia-summary-content-urls-mobile
          wikimedia-summary-dir wikimedia-summary-displaytitle wikimedia-summary-extract wikimedia-summary-extract-html
          wikimedia-summary-lang wikimedia-summary-namespace wikimedia-summary-pageid wikimedia-summary-revision
          wikimedia-summary-tid wikimedia-summary-timestamp wikimedia-summary-title wikimedia-summary-titles
          wikimedia-summary-type wikimedia-summary-wikibase-item wikimedia-summary-description
          wikipedia-image wikimedia-summary-originalimage wikimedia-summary-thumbnail
          ;; For getting the page in html format: (string)
          wikimedia-page-html
          ;; For getting a list of related pages (wikimedia-summaries):
          wikimedia-related-pages))

;;=========================================================================
;; BASIC FUNCTION FOR ACCESSING API ENDPOINTS
;;=========================================================================
;;
;; Read the API documentation here: https://en.wikipedia.org/api/rest_v1/

(defun wikimedia-rest-api (api-endpoint
                           &key
                           (language "en")
                           (user-agent *user-agent*)
                           (method :get)
                           (lisp-format :alist)) ;; Turn to hashtable for faster performance
  "General function to interact with Wikimedia-rest-api for requesting JSON objects."
  (let* ((uri (format nil "https://~a.wikipedia.org/api/rest_v1/~a" language api-endpoint))
         (response-stream (drakma:http-request uri
                                               :user-agent user-agent
                                               :method method
                                               :content-type "application/json"
                                               :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format response-stream) :utf-8)
    (yason:parse response-stream :object-as lisp-format)))
;; Examples:
;; (wikimedia-rest-api "page/") ;; A list of page-related API endpoints
;; (wikimedia-rest-api "page/title/Fluid_construction_grammar") ;; Get revision metadata for a title
;; (wikimedia-rest-api "page/title/Luc_Steels" :language "fr")

;;=========================================================================
;; GETTING SUMMARIES
;;=========================================================================

(defun normalize-title (title)
  "Replace spaces with underscores."
  (cl-ppcre:regex-replace-all " " title "_"))

;; TODO: Handle error codes
(defun wikimedia-summary (title &key (language "en") (lisp-format :hash-table))
  "The summary of a given page. Usage of hash-table is advised."
  (wikimedia-rest-api (format nil "page/summary/~a" (normalize-title title))
                      :lisp-format lisp-format
                      :language language))
;; (wikimedia-summary "Fluid construction grammar")
;; (wikimedia-summary "Fluid construction grammar" :lisp-format :alist) ;; Not recommended

;;;; If you obtained the summary as a hash-table, you can simply access all of the 
;;;; summaries' data using the hash keys if you know them. For example:
;; (gethash "extract" (wikimedia-summary "Fluid_construction_grammar")) ; plain text summary
;; (gethash "extract_html" (wikimedia-summary "Fluid_construction_grammar")) ; html-formatted summary

;;;; Alternatively, if you do not know all keys by heart, you can use the following helper functions.
;;;; Simply type wikimedia-summary- and check auto-completion to see which functions exist.
;;;; -------------------------------------------------------------------------------------------------
(defun wikimedia-summary-type (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "type" wikimedia-summary))
;; (wikimedia-summary-type (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-title (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "title" wikimedia-summary)) 
;; (wikimedia-summary-title (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-description (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "description" wikimedia-summary))
;; (wikimedia-summary-description (wikimedia-summary "Sony"))
;; (wikimedia-summary-description (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-displaytitle (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "displaytitle" wikimedia-summary))
;; (wikimedia-summary-displaytitle (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-namespace (wikimedia-summary &key key)
  (assert (hash-table-p wikimedia-summary))
  (multiple-value-bind (namespace exists)
      (gethash "namespace" wikimedia-summary)
    (if (hash-table-p namespace)
      (case key
        (:id (gethash "id" namespace))
        (:text (gethash "text" namespace))
        (t
         (values namespace exists)))
      (values namespace exists))))
;; (wikimedia-summary-namespace (wikimedia-summary "Fluid construction grammar"))
;;;; For accessing the subfields:
;; (wikimedia-summary-namespace (wikimedia-summary "Fluid construction grammar") :key :id)
;; (wikimedia-summary-namespace (wikimedia-summary "Fluid construction grammar") :key :text)

(defun wikimedia-summary-wikibase-item (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "wikibase_item" wikimedia-summary))
;; (wikimedia-summary-wikibase-item (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-titles (wikimedia-summary &key key)
  (assert (hash-table-p wikimedia-summary))
  (multiple-value-bind (titles exists)
      (gethash "titles" wikimedia-summary)
    (if (hash-table-p titles)
      (case key
        (:canonical (gethash "canonical" titles))
        (:normalized (gethash "normalized" titles))
        (:display (gethash "display" titles))
        (t
         (values titles exists)))
      (values titles exists))))
;; (wikimedia-summary-titles (wikimedia-summary "Fluid construction grammar"))
;;;; For accessing the subfields"
;; (wikimedia-summary-titles (wikimedia-summary "Fluid construction grammar") :key :canonical)
;; (wikimedia-summary-titles (wikimedia-summary "Fluid construction grammar") :key :normalized)
;; (wikimedia-summary-titles (wikimedia-summary "Fluid construction grammar") :key :display)

(defun wikimedia-summary-pageid (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "pageid" wikimedia-summary))
;; (wikimedia-summary-pageid (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-lang (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "lang" wikimedia-summary)) 
;; (wikimedia-summary-lang (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-dir (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "dir" wikimedia-summary))
;; (wikimedia-summary-dir (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-revision (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "revision" wikimedia-summary))
;; (wikimedia-summary-revision (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-tid (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "tid" wikimedia-summary)) 
;; (wikimedia-summary-tid (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-timestamp (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "timestamp" wikimedia-summary))
;; (wikimedia-summary-timestamp (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-content-urls (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "content_urls" wikimedia-summary)) 
;; (wikimedia-summary-content-urls (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-content-urls-desktop (wikimedia-summary &key key)
  (multiple-value-bind (hash exists)
      (gethash "desktop" (wikimedia-summary-content-urls wikimedia-summary))
    (if (hash-table-p hash)
      (case key
        (:page (gethash "page" hash))
        (:revisions (gethash "revisions" hash))
        (:edit (gethash "edit" hash))
        (:talk (gethash "talk" hash))
        (t
         (values hash exists)))
      (values hash exists))))
;; (wikimedia-summary-content-urls-desktop (wikimedia-summary "Fluid construction grammar"))
;; (wikimedia-summary-content-urls-desktop (wikimedia-summary "Fluid construction grammar") :key :page)
;; (wikimedia-summary-content-urls-desktop (wikimedia-summary "Fluid construction grammar") :key :revisions)
;; (wikimedia-summary-content-urls-desktop (wikimedia-summary "Fluid construction grammar") :key :edit)
;; (wikimedia-summary-content-urls-desktop (wikimedia-summary "Fluid construction grammar") :key :talk)

(defun wikimedia-summary-content-urls-mobile (wikimedia-summary &key key)
  (multiple-value-bind (hash exists)
      (gethash "mobile" (wikimedia-summary-content-urls wikimedia-summary))
    (if (hash-table-p hash)
      (case key
        (:page (gethash "page" hash))
        (:revisions (gethash "revisions" hash))
        (:edit (gethash "edit" hash))
        (:talk (gethash "talk" hash))
        (t
         (values hash exists)))
      (values hash exists))))
;; (wikimedia-summary-content-urls-mobile (wikimedia-summary "Fluid construction grammar"))
;; (wikimedia-summary-content-urls-mobile (wikimedia-summary "Fluid construction grammar") :key :page)
;; (wikimedia-summary-content-urls-mobile (wikimedia-summary "Fluid construction grammar") :key :revisions)
;; (wikimedia-summary-content-urls-mobile (wikimedia-summary "Fluid construction grammar") :key :edit)
;; (wikimedia-summary-content-urls-mobile (wikimedia-summary "Fluid construction grammar") :key :talk)

(defun wikimedia-summary-extract (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "extract" wikimedia-summary))
;; (wikimedia-summary-extract (wikimedia-summary "Fluid construction grammar"))

(defun wikimedia-summary-extract-html (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary)) 
  (gethash "extract_html" wikimedia-summary))
;; (wikimedia-summary-extract-html (wikimedia-summary "Fluid construction grammar"))

(defstruct wikimedia-image height width source)

(defun wikimedia-summary-originalimage (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary))
  (let ((image-spec (gethash "originalimage" wikimedia-summary)))
    (when image-spec
      (make-wikimedia-image
       :height (format nil "~a" (gethash "height" image-spec))
       :width (format nil "~a" (gethash "width" image-spec))
       :source (gethash "source" image-spec)))))

(defun wikimedia-summary-thumbnail (wikimedia-summary) 
  (assert (hash-table-p wikimedia-summary))
  (let ((image-spec (gethash "thumbnail" wikimedia-summary)))
    (when image-spec
      (make-wikimedia-image
       :height (format nil "~a" (gethash "height" image-spec))
       :width (format nil "~a" (gethash "width" image-spec))
       :source (gethash "source" image-spec)))))

;;=========================================================================
;; GETTING HTML OF A PAGE
;;=========================================================================

(defun wikimedia-page-html (title
                            &key (language "en"))
  (let ((uri (format nil "https://~a.wikipedia.org/api/rest_v1/page/html/~a" language (normalize-title title))))
    (drakma:http-request uri)))
;; (wikimedia-page-html "Luc Steels")

;;=========================================================================
;; GETTING RELATED PAGES
;;=========================================================================

(defun wikimedia-related-pages (title 
                                &key (language "en")
                                (lisp-format :hash-table))
  "Returns a list of summaries as hash-tables, plists or as alists."
  (let ((result (wikimedia-rest-api (format nil "page/related/~a" (normalize-title title))
                                    :language language
                                    :lisp-format lisp-format)))
    (case lisp-format
      (:hash-table (gethash "pages" result))
      (:alist (rest (assoc "pages"  result :test #'string=)))
      (:plist result)
      (t
       (error (format nil "The lisp-format ~a is not supported by wikimedia-related-pages" lisp-format))))))  
;; (wikimedia-related-pages "Luc Steels" :lisp-format :alist)