(ql:quickload :geoquery-lsfb)
(in-package :geoquery-lsfb)

;(monitors::activate-monitor trace-fcg)

;;--------------------;;
;; + Configurations + ;;
;;--------------------;;

; global variable determining which is the dominant hand ('RH or 'LH)
(defparameter *dominant-hand*
  'RH)

; global variable determining where the data for the experiment is stored
(defparameter *data-folder*
  "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data")

;;------------------------;;
;; + elan-to-predicates + ;;
;;------------------------;;

;; To create a set of predicates from an elan-file (xml), read in
;; the file using read-xml and pass it to elan->predicates

;(pprint (elan->predicates (read-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/95_0_876.eaf")))



;;------------------------------;;
;; + Generating dataset files + ;;
;;------------------------------;;

;; loading in background data needed to generate dataset files
(load-data-for-generation *data-folder*)

;;---------------------------;;
;; + Reading dataset-files + ;;
;;---------------------------;;

;(jsonl->list-of-json-alists "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-250.jsonl")