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
(in-package :fcg)

;(export '(:followed-by-front-vowel))

(defmethod fcg-expand ((type (eql :followed-by-front-vowel?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("e" "é" "i" "í")))
        (if (member (first source) front-vowels :test #'string=)
          source
          nil)))))

(defmethod fcg-expand ((type (eql :stressed-stem-assimilation?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("o" "a" "ó" "á")))
        (if (and (member (first source) front-vowels :test #'string=)
                 (= (length source) 1))
          source
          nil)))))

(defmethod fcg-expand ((type (eql :unstressed-stem-assimilation?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("o" "a" "ó" "á")))
        (if (and (member (first source) front-vowels :test #'string=)
                 (> (length source) 1))
          source
          nil)))))


(defmethod fcg-expand ((type (eql :stressed-stem?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (if (= (length source) 1)
        source
        nil))))

(defmethod fcg-expand ((type (eql :unstressed-stem?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (if (= (length source) 1) ;;not length but look inside strings
        source
        nil))))


