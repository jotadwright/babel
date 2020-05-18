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

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data t :ignore-stored-data nil)
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


(defun all-sentences-annotated-with-roleset (roleset)
  (loop for sentence in (train-split *propbank-annotations*)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'string=)
        collect sentence))

;; (length (all-sentences-annotated-with-roleset "say.01"))



(defun learn-cxn-from-propbank-annotation (propbank-sentence roleset)
  (let ((frame (find roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'string=))
        (syntactic-analysis (nlp-tools:get-penelope-syntactic-analysis (sentence-string propbank-sentence))))
    syntactic-analysis

  ))

;; (learn-cxn-from-propbank-annotation *believe-sentence* "believe.01")

(setf *believe-sentence* (second (all-sentences-annotated-with-roleset "believe.01")))

(all-sentences-annotated-with-roleset "believe.01")