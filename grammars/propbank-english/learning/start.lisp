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

;; (length (all-sentences-annotated-with-roleset "believe.01"))


(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun make-propbank-contributing-unit (units-with-role frame)
  "Make a contributing unit based on a frame and units-with-role."
  (let* ((v-unit (cdr (assoc "V" units-with-role :key #'role-type :test #'string=)))
         (unit-name (variablify (unit-name v-unit)))
         (args (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect '(referent ?f)
                     else collect `(,(make-kw (role-type r)) ,(variablify (unit-name (cdr (assoc r units-with-role)))))))
         (meaning (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect `(frame ,(intern (upcase (frame-name frame))) ?f)
                     else collect `(frame-element ,(intern (upcase (role-type r))) ?f
                                                  ,(variablify (unit-name (cdr (assoc r units-with-role))))))))
    `(,unit-name
      (args ,@args)
      (frame-evoking +)
      (meaning ,meaning))))

               
       
(defun learn-cxn-from-propbank-annotation (propbank-sentence roleset)
  (let* ((frame (find roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'string=))
         (initial-transient-structure-and-unit-name-ids
          (multiple-value-list (de-render (sentence-string propbank-sentence) :de-render-constituents-dependents)))
         (units-with-role (loop for role in (frame-roles frame)
                                for role-start = (first (indices role))
                                for role-end = (+ (last-elt (indices role)) 1)
                                for unit = (find-unit-by-span
                                            (left-pole-structure (first initial-transient-structure-and-unit-name-ids))
                                            (list role-start role-end))
                                collect (cons role unit)))
         (contributing-unit (make-propbank-contributing-unit units-with-role frame)))
    contributing-unit))

;; (learn-cxn-from-propbank-annotation *believe-sentence* "believe.01")

(setf *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

(all-sentences-annotated-with-roleset "believe.01")



(def-fcg-cxn believe.01-np-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np))) 
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))