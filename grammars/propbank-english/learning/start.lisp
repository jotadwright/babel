;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
(cl-propbank:load-pb-data :store-data t :ignore-stored-data nil)
(length cl-propbank:*pb-data*)

;; Loading the Propbank annotations (takes a minute to two minutes)
(load-propbank-annotations :store-data t :ignore-stored-data nil)
(length *propbank-annotations*)

