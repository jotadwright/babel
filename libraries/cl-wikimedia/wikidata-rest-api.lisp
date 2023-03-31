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

;; This file contains some convenience functions that use the basic 
;; interface specified in wikimedia-action-api.lisp. Note that these
;; assume that you store the entity's information as a hash-table.
;; All helper functions are preceded by the prefix wd- (wikidata-).

(export '(wd-entity-id
          wd-entity-alias wd-entity-aliases
          wd-entity-description wd-entity-descriptions
          wd-entity-label wd-entity-labels
          wd-entity-sitelinks
          wd-entity-wikipedia-title wd-entity-wikipedia-page wd-entity-wikipedia-summary))

;; The commented examples assume the following wikidata entity:
;; (defparameter *venus* (wikidata-get-entity "Q47652" :user-agent "YOURUSERAGENT" :lisp-format :hash-table))

;; Supporting function
;; ------------------------------------------------------------------------ 
(defun wd-return-value (hash-table return-as-list-p)
  "Returns the hash-table as a list of keys and values if return-as-list-p is T, otherwise return hash-table." 
  (if return-as-list-p
    (loop for key being each hash-key of hash-table
            using (hash-value v)
            collect (list key v))
    hash-table))

;; Interface functions
;; ------------------------------------------------------------------------ 
(defun wd-entity-id (wd-entity)
  "Return the ID of a wd-entity (string)"
  (assert (hash-table-p wd-entity))
  (gethash "id" wd-entity))
;; (wd-entity-id *venus*)

(defun wd-entity-aliases (wd-entity &key (return-as-list-p nil))
  "Return the aliases of a wikidata-entity as hash-table or list."
  (assert (hash-table-p wd-entity))
  (let ((aliases (gethash "aliases" wd-entity)))
    (wd-return-value aliases return-as-list-p)))
;; (wd-entity-aliases *venus*)
;; (wd-entity-aliases *venus* :return-as-list-p t)

(defun wd-entity-alias (wd-entity language)
  "Get a language-specific alias of a wikidata entity (string)" 
  (gethash language (wd-entity-aliases wd-entity)))
;; (wd-entity-alias *venus* "nl")

(defun wd-entity-descriptions (wd-entity &key (return-as-list-p nil))
  "Return the descriptions of a wikidata-entity as hash-table or list."
  (assert (hash-table-p wd-entity))
  (let ((descriptions (gethash "descriptions" wd-entity)))
    (wd-return-value descriptions return-as-list-p)))
;; (wd-entity-descriptions *venus*)
;; (wd-entity-descriptions *venus* :return-as-list-p t)

(defun wd-entity-description (wd-entity language)
  "Get a language-specific label of a wikidata entity (string)" 
  (gethash language (wd-entity-descriptions wd-entity)))
;; (wd-entity-description *venus* "en")

(defun wd-entity-labels (wd-entity &key (return-as-list-p nil))
  "Return the labels of a wikidata-entity as hash-table or list."
  (assert (hash-table-p wd-entity))
  (let ((the-labels (gethash "labels" wd-entity)))
    (if return-as-list-p
      (loop for key being each hash-key of the-labels
              using (hash-value v)
            collect (list key v))
      the-labels)))
;; (wd-entity-labels *venus*)
;; (wd-entity-labels *venus* :return-as-list-p t)

(defun wd-entity-label (wd-entity language)
  "Get a language-specific label of a wikidata entity (string)." 
  (gethash language (wd-entity-labels wd-entity)))
;; (wd-entity-label *venus* "en")

(defun wd-entity-statements (wd-entity &key (return-as-list-p nil))
  "Obtain the statements associated with a wikidata entity."
  (assert (hash-table-p wd-entity))
  (let ((statements (gethash "statements" wd-entity)))
    (wd-return-value statements return-as-list-p)))
;; (wd-entity-statements *venus*)
;; (wd-entity-statements *venus* :return-as-list-p t)

(defun wd-entity-sitelinks (wd-entity &key (return-as-list-p nil))
  "Obtain the statements associated with a wikidata entity."
  (assert (hash-table-p wd-entity))
  (let ((sitelinks (gethash "sitelinks" wd-entity)))
    (wd-return-value sitelinks return-as-list-p)))
;; (wd-entity-sitelinks *venus*)

(defun wd-entity-wikipedia-title (wd-entity language)
  "Get the title of a corresponding wikipedia article (if it exists)"
  (assert (hash-table-p wd-entity))
  (let* ((key (format nil "~awiki" language))
         (entry (gethash key (wd-entity-sitelinks wd-entity))))
    (when entry
      (gethash "title" entry))))
;; (wd-entity-wikipedia-title *venus* "nl")
;; (wikimedia-summary (wd-entity-wikipedia-title *venus* "nl") :language "nl" :lisp-format :alist)

(defmacro wd-entity-wikipedia-page (wd-entity language &rest parameters &key &allow-other-keys)
  `(let ((title (wd-entity-wikipedia-title ,wd-entity ,language)))
     (when title
       (wikipedia-parse :page title :language ,language ,@parameters))))
;; (wd-entity-wikipedia-page *venus* "en")

(defmacro wd-entity-wikipedia-summary (wd-entity language &rest parameters &key &allow-other-keys)
  `(let ((title (wd-entity-wikipedia-title ,wd-entity ,language)))
     (when title
       (wikimedia-summary title :language ,language ,@parameters))))
;; (wd-entity-wikipedia-summary *venus* "en" :lisp-format :alist)
;; (wikimedia-summary-extract (wd-entity-wikipedia-summary *venus* "en"))
;; (wikimedia-summary-thumbnail (wd-entity-wikipedia-summary *venus* "en")) 

