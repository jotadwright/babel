;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
(load-pb-data :store-data t :ignore-stored-data nil)
(length *pb-data*)

;; Loading the Propbank annotations (takes a minute to two minutes)
(load-propbank-annotations :store-data t :ignore-stored-data t)
(length (train-split *propbank-annotations*))




(defun all-rolesets-for-framenet-frame (framenet-frame-name)
  (loop for predicate in *pb-data*
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)


#|
(defun all-sentences-annotated-with-roleset (roleset)
  (loop for sentence in *propbank-
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)
 |#