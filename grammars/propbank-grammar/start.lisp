;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Learning and evaluating PropBank-based  grammars. ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;

;; Loading the :propbank-english system
;;(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)


;; Activating spacy-api locally
(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;; Loading the Propbank annotations (takes a couple of minutes)
(load-propbank-annotations 'ewt :ignore-stored-data nil)
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)
; *ewt-annotations*
; *ontonotes-annotations*

 