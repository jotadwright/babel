;;***********************;;
;;                       ;;
;; Using the SLP-package ;;
;;                       ;;
;;***********************;;

;; Loading the package using quickload:
(ql:quickload :slp)

;; Intern new symbols within the slp-package:
(in-package :slp)


;;;;;;;;;;;;;;;;;;
;; loading data ;;
;;;;;;;;;;;;;;;;;;

;;!! Be sure to follow the steps indicated in the readme of this package to download the necessary data first!!;;

;; set pointer to the GeoQuery-LSFB data folder:
(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

;; to load an example, point to its location and use read-xml function:
;; loading example utterance 1:
(defparameter *test-utterance-xml-1*
  (read-xml
  (merge-pathnames
   (make-pathname :directory '(:relative "elan-files")
                  :name "1_0_1" :type "eaf")
   *geoquery-lsfb-data*)))

;; loading example utterance 2:
(defparameter *test-utterance-xml-2*
  (read-xml
  (merge-pathnames
   (make-pathname :directory '(:relative "elan-files")
                  :name "2_0_2" :type "eaf")
   *geoquery-lsfb-data*)))

;; loading example utterance 3:
(defparameter *test-utterance-xml-3*
  (read-xml
  (merge-pathnames
   (make-pathname :directory '(:relative "elan-files")
                  :name "3_0_10" :type "eaf")
   *geoquery-lsfb-data*)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; elan-to-predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; A module for transforming xml files created by the elan-software into the predicate notation used in FCG to represent signed forms. The elan-files should be created using the provided template. 

;; xml-structures of utterances are transformed into predicates using elan->predicates (input should be xmls-node):
(defparameter *test-utterance-1-predicates*
 (elan->predicates *test-utterance-xml-1*))
;(pprint  (predicates *test-utterance-1-predicates*))

(defparameter *test-utterance-2-predicates*
 (elan->predicates *test-utterance-xml-2*))
;(pprint  (predicates *test-utterance-2-predicates*))

(defparameter *test-utterance-3-predicates*
 (elan->predicates *test-utterance-xml-3*))
;(pprint  (predicates *test-utterance-3-predicates*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create fingerspelled forms (LSFB) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create a fingerspelled form in hamnosys (LSFB alphabet) for a string
(make-fingerspelling "arizona")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; representing time alignment of predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-element (make-html *test-utterance-1-predicates*))
(add-element `((hr)))
(add-element (make-html *test-utterance-2-predicates*))
(add-element `((hr)))
(add-element (make-html *test-utterance-3-predicates*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading GeoQuery-LSFB corpus files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XML versions
(defparameter *geoquery-lsfb-250-xml*
  (load-geoquery-corpus-xml
   (merge-pathnames
    (make-pathname :directory '(:relative "xml-files")
                   :name "geoquery-lsfb-250" :type "xml")
    *geoquery-lsfb-data*)))

(defparameter *geoquery-lsfb-4500-xml*
  (load-geoquery-corpus-xml
   (merge-pathnames
    (make-pathname :directory '(:relative "xml-files")
                   :name "geoquery-lsfb-4500" :type "xml")
    *geoquery-lsfb-data*)))

;; json  versions
(defparameter *geoquery-lsfb-250-jsonl*
  (load-geoquery-corpus-jsonl
   (merge-pathnames
    (make-pathname :directory '(:relative "json-files")
                   :name "geoquery-lsfb-250" :type "jsonl")
    *geoquery-lsfb-data*)))

(defparameter *geoquery-lsfb-4500-jsonl*
  (load-geoquery-corpus-jsonl
   (merge-pathnames
    (make-pathname :directory '(:relative "json-files")
                   :name "geoquery-lsfb-4500" :type "jsonl")
    *geoquery-lsfb-data*)))











